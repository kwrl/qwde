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
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.io.FilenameUtils;

import qwde.models.StockPrice;
import qwde.util.StockPriceReader;

public class PystockStockPriceReader implements StockPriceReader {
  private final List<StockPrice> stockPrices;
  public final static DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd");

  private boolean fileNameMatchesDate(File file, Set<LocalDate> dates) {
    LocalDate fileNameAsDate = LocalDate.parse(FilenameUtils.getBaseName(file.getName()), dateTimeFormatter);
    return dates.contains(fileNameAsDate);
  }

  public PystockStockPriceReader(Set<LocalDate> desirableDates) throws IOException {
    this.stockPrices = Files.walk(new File("../../..").toPath())
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
            })
            .parallel()
            .flatMap(PystockStockPriceReader::getPricesFromCompressedArchive)
            .collect(Collectors.toList());
  }

  public PystockStockPriceReader(InputStream stockPriceFile) throws IOException {
    this.stockPrices = getPricesFromCompressedArchive(stockPriceFile).collect(Collectors.toList());
  }

  public static PystockStockPriceReader FromDate(LocalDate date) throws IOException {
    DateTimeFormatter yearFormatter = DateTimeFormatter.ofPattern("yyyy");

    ClassLoader classLoader = PystockStockPriceReader.class.getClassLoader();
    InputStream resourceStream = classLoader.getResourceAsStream(String.format("pystock-data/%s/%s.tar.gz",
            date.format(yearFormatter),
            date.format(PystockStockPriceReader.dateTimeFormatter)));
    if (resourceStream == null) {
      throw new FileNotFoundException(String.format("Could not find file '%s'", date));
    }

    return new PystockStockPriceReader(resourceStream);
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

          reader.close();
          return prices.stream();
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
    return this.stockPrices;
  }
}
