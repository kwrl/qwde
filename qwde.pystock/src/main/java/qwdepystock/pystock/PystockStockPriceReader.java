package qwdepystock.pystock;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Comparator;
import java.util.List;
import java.util.HashSet;
import java.util.ArrayList;
import java.util.Set;
import java.util.Optional;
import java.util.TreeSet;
import java.util.Collections;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.io.FilenameUtils;

import qwdepystock.models.StockPrice;
import qwdepystock.pystock.PystockStockPrice;
import qwdepystock.util.StockPriceReader;
import qwdepystock.util.DateUtil;
import qwdepystock.util.FileUtil;

public class PystockStockPriceReader implements StockPriceReader {
  private static Logger logger = LoggerFactory.getLogger(PystockStockPriceReader.class);

  private final List<StockPrice> stockPrices;
  public final static DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd");

  private static boolean fileNameMatchesDate(String fileName, Set<LocalDate> dates) {
    try {
      LocalDate fileNameAsDate = LocalDate.parse(fileName, dateTimeFormatter);
      return dates.contains(fileNameAsDate);
    } catch (DateTimeParseException exception) {
      return false;
    }
  }

  private static Set<LocalDate> getDateRange(LocalDate startDate, LocalDate endDate) {
    return startDate.datesUntil(endDate).filter(d -> !(d.getDayOfWeek() == DayOfWeek.SATURDAY || d.getDayOfWeek() == DayOfWeek.SUNDAY)).collect(Collectors.toSet());
  }

  public PystockStockPriceReader(Predicate<String> fileNameFilter) throws IOException {
    try {
      this.stockPrices = Files.walk(Path.of(FileUtil.getGitRootDirectory(), "pystock-data"))
        .map(Path::toFile)
        .filter(File::isFile)
        .filter(file -> file.getName().endsWith(".tar.gz"))
        .filter(f -> fileNameFilter.test(FilenameUtils.getBaseName(FilenameUtils.getBaseName(f.getName()))))
        .map(file -> {
          try {
            logger.trace("Parsing {}", file);
            return new FileInputStream(file);
          } catch (FileNotFoundException exception) {
            throw new UncheckedIOException(exception);
          }
        })
        .flatMap(gz -> {
          try {
            return getPricesFromCompressedArchive(gz).stream();
          } catch (IOException e) {
            throw new UncheckedIOException(e);
          }
        })
      .sorted().distinct()
      .collect(Collectors.toList());

      logger.info("{} entries loaded in memory from pystock-data", this.stockPrices.size());
    } catch (UncheckedIOException exception) {
      throw new IOException(exception);
    }
  }

  public static PystockStockPriceReader getPystockStockPriceReader(LocalDate startDate, LocalDate endDate) throws IOException {
    Set<LocalDate> desirableDates = getDateRange(startDate, endDate.plusDays(1));
    return new PystockStockPriceReader(f -> fileNameMatchesDate(f, desirableDates));
  }

  public static PystockStockPriceReader FromDate(LocalDate date) throws IOException {
    return getPystockStockPriceReader(date, date);
  }

  private static List<StockPrice> getPricesFromCompressedArchive(InputStream compressedArchive) throws IOException {
    TarArchiveInputStream stream = new TarArchiveInputStream(new GzipCompressorInputStream(compressedArchive));
    TarArchiveEntry entry;
    while ((entry = stream.getNextTarEntry()) != null) {
      if ("prices.csv".equals(entry.getName())) {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(stream))) {
          // Skip header
          return reader.lines().skip(1).map(PystockStockPriceReader::parseStockPrice).filter(Optional::isPresent).map(Optional::get).collect(Collectors.toList());
        }
      }
    } 

    return Collections.emptyList();
  }

  private static Optional<StockPrice> parseStockPrice(String line) {
    if (line.startsWith("symbol")) {
      return Optional.empty();
    }
    String[] tokens = line.split(",");
    return Optional.of(new PystockStockPrice(new BigDecimal(tokens[3]), new BigDecimal(tokens[4]), new BigDecimal(tokens[5]), tokens[0], parseTimestamp(tokens[1])));
  }

  private static LocalDateTime parseTimestamp(String line) {
    String[] tokens = line.split("-");
    return LocalDateTime.of(Integer.parseInt(tokens[0]), Integer.parseInt(tokens[1]), Integer.parseInt(tokens[2]), 0, 0, 0, 0);
  }

  @Override
  public List<StockPrice> read() {
    return this.stockPrices;
  }
}
