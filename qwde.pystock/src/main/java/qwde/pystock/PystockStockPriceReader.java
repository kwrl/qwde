package qwde.pystock;

import java.io.BufferedInputStream;
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
import java.nio.file.Paths;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.io.FilenameUtils;

import qwde.models.StockPrice;
import qwde.pystock.PystockStockPrice;
import qwde.util.StockPriceReader;

public class PystockStockPriceReader implements StockPriceReader {
  private static Logger logger = LoggerFactory.getLogger(PystockStockPriceReader.class);

  private final List<StockPrice> stockPrices;
  public final static DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd");

  private boolean fileNameMatchesDate(File file, Set<LocalDate> dates) {
    LocalDate fileNameAsDate = LocalDate.parse(FilenameUtils.getBaseName(FilenameUtils.getBaseName(file.getName())), dateTimeFormatter);
    return dates.contains(fileNameAsDate);
  }

  private static String getGitRootDirectory() throws IOException {
    ProcessBuilder builder = new ProcessBuilder("git", "rev-parse", "--show-toplevel");
    builder.redirectErrorStream(true);
    Process process = builder.start();
    InputStream is = process.getInputStream();
    BufferedReader reader = new BufferedReader(new InputStreamReader(is));

    return reader.readLine();
  }

  // TODO: holidays are skipped since they are not found, but we inform the logger about it
  private static Set<LocalDate> getDateRange(LocalDate startDate, LocalDate endDate) {
    return startDate.datesUntil(endDate).filter(d -> !(d.getDayOfWeek() == DayOfWeek.SATURDAY || d.getDayOfWeek() == DayOfWeek.SUNDAY)).collect(Collectors.toSet());
  }

  public PystockStockPriceReader(LocalDate startDate, LocalDate endDate) throws IOException {
    Set<LocalDate> desirableDates = getDateRange(startDate, endDate.plusDays(1));
    Set<Integer> years = desirableDates.stream().map(date -> date.getYear()).collect(Collectors.toSet());
    Stream<Path> yearPaths = years.stream()
      .map(year -> {
        try {
          Path yearPath = Path.of(getGitRootDirectory(), "pystock-data",  year.toString());
          return yearPath;
        } catch (IOException exception) {
          throw new UncheckedIOException(exception);
        }
      });
    this.stockPrices = yearPaths.flatMap(yearPath -> {
      try {
        return Files.walk(yearPath)
          .map(path -> path.toFile())
          .filter(File::isFile)
          .filter(file -> file.getName().endsWith(".tar.gz"))
          .filter(file -> fileNameMatchesDate(file, desirableDates))
          .map(file -> {
            try {
              return new FileInputStream(file);
            } catch (FileNotFoundException exception) {
              throw new UncheckedIOException(exception);
            }
          });
      } catch (IOException exception) {
        throw new UncheckedIOException(exception);
      }})
      .parallel()
      .flatMap(PystockStockPriceReader::getPricesFromCompressedArchive)
      .collect(Collectors.toList());
  }

  public PystockStockPriceReader(InputStream stockPriceFile) throws IOException {
    this.stockPrices = getPricesFromCompressedArchive(stockPriceFile).collect(Collectors.toList());
  }

  public static PystockStockPriceReader FromDate(LocalDate date) throws IOException {
    return new PystockStockPriceReader(date, date);
  }

  public static Stream<StockPrice> getPricesFromCompressedArchive(InputStream compressedArchive) {
    try {
      TarArchiveInputStream stream = new TarArchiveInputStream(new GzipCompressorInputStream(compressedArchive));
      TarArchiveEntry entry;
      while ((entry = stream.getNextTarEntry()) != null) {
        if ("prices.csv".equals(entry.getName())) {
          BufferedReader reader = new BufferedReader(new InputStreamReader(stream));
          List<StockPrice> prices = reader.lines()
            .map(line -> {
              try {
                return parseStockPrice(line);
              } catch (Exception e) {
                return null;
              }
            })
          .filter(x -> x != null)
            .collect(Collectors.toList());
          // there are redundant entries in some files, with another date. I don't know why.
          Set<StockPrice> filteredPrices = new TreeSet<>(new Comparator<StockPrice>() {
            @Override
            public int compare(StockPrice left, StockPrice right) {
              return left.getCompany().equals(right.getCompany()) ? 0 : 1;
            }});

          filteredPrices.addAll(prices);

          reader.close();
          return filteredPrices.stream();
        }
      }
      stream.close();
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    } catch (IOException e) {
      e.printStackTrace();
    }

    return Stream.empty();
  }

  private static StockPrice parseStockPrice(String line) {
    String[] tokens = line.split(",");
    return new PystockStockPrice(new BigDecimal(tokens[3]), new BigDecimal(tokens[4]), tokens[0], parseTimestamp(tokens[1]));
  }

  private static LocalDateTime parseTimestamp(String line) {
    String[] tokens = line.split("-");
    return LocalDateTime.of(Integer.parseInt(tokens[0]), Integer.parseInt(tokens[1]), Integer.parseInt(tokens[2]), 0, 0, 0, 0);
  }

  @Override
  public List<StockPrice> read() {
    logger.info("I have " + this.stockPrices.size() + " entries");
    return this.stockPrices;
  }
}
