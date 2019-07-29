package qwde.pystock;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;

import qwde.models.StockPrice;
import qwde.util.StockPriceReader;

public class PystockStockPriceReader implements StockPriceReader {
  private final List<StockPrice> stockPrices;

  public PystockStockPriceReader(Path directoryPath) throws IOException {
    this.stockPrices = Files.walk(directoryPath)
        .map(path -> path.toFile())
         .filter(File::isFile)
         .filter(file -> file.getName().endsWith(".tar.gz"))
         .parallel()
         .flatMap(PystockStockPriceReader::getPricesFromCompressedArchive)
         .collect(Collectors.toList());
  }

  public PystockStockPriceReader(File stockPriceFile) throws IOException {
    this.stockPrices = getPricesFromCompressedArchive(stockPriceFile).collect(Collectors.toList());
  }

  public static PystockStockPriceReader FromDate(String date) throws IOException {
    ClassLoader classLoader = PystockStockPriceReader.class.getClassLoader();
    return new PystockStockPriceReader(new File(classLoader.getResource(date).getFile()));
  }

  public static Stream<StockPrice> getPricesFromCompressedArchive(File compressedArchive) {
    try {
      TarArchiveInputStream stream = new TarArchiveInputStream(new GzipCompressorInputStream(new BufferedInputStream(new FileInputStream(compressedArchive))));
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
