package qwde.dataprovider.pystock;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.dataprovider.models.IStockTicker;
import qwde.dataprovider.models.StockTicker;
import qwde.dataprovider.util.FileUtil;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class PystockDataReader {
    private static final Logger LOG = LoggerFactory.getLogger(PystockDataReader.class);

    private static final String DATA_FOLDER = "pystock-data";
    public static final DateTimeFormatter DATETIMEFORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");

    private PystockDataReader() {
    }

    private static boolean fileNameMatchesDate(String fileName, Set<LocalDate> dates) {
        try {
            LocalDate fileNameAsDate = LocalDate.parse(fileName, DATETIMEFORMATTER);
            return dates.contains(fileNameAsDate);
        } catch (DateTimeParseException exception) {
            return false;
        }
    }

    private static Set<LocalDate> getDateRange(LocalDate startDate, LocalDate endDate) {
        return startDate.datesUntil(endDate).filter(d -> !(d.getDayOfWeek() == DayOfWeek.SATURDAY || d.getDayOfWeek() == DayOfWeek.SUNDAY)).collect(Collectors.toSet());
    }

    @SuppressWarnings("PMD.ExceptionAsFlowControl")
    public static Stream<IStockTicker> read(Predicate<String> fileNameFilter) throws IOException {
        // Not pretty, but it covers most use-cases by other devs.
        // I.e., we search in XDG_DATA_HOME, XDG_DATA_DIRS, and the current working directory, and the directory above
        Optional<Path> pystockDataPath = Stream.of(FileUtil.findFolderInDatapath(DATA_FOLDER), FileUtil.findInPath(DATA_FOLDER, "."), FileUtil.findInPath(DATA_FOLDER, ".."), FileUtil.findInPath(DATA_FOLDER, "../dataprovider"), FileUtil.findInPath(DATA_FOLDER, "./dataprovider")).filter(Optional::isPresent).map(Optional::get).findFirst();
        if (pystockDataPath.isEmpty()) {
            throw new FileNotFoundException("Unable to find pystock-data files. See README.md for more documentation.");
        }
        LOG.debug("Located pystock-data in {}", pystockDataPath.get());
        try {
            LOG.info("Reading pystock-data...");
            return Files.walk(pystockDataPath.get())
                  .map(Path::toFile)
                  .filter(File::isFile)
                  .filter(file -> file.getName().endsWith(".tar.gz"))
                  .filter(f -> fileNameFilter.test(FilenameUtils.getBaseName(FilenameUtils.getBaseName(f.getName()))))
                  .map(file -> {
                      try {
                          LOG.debug("Parsing {}", file);
                          return Files.newInputStream(file.toPath());
                      } catch (IOException exception) {
                          throw new UncheckedIOException(exception);
                      }
                  })
                  .flatMap(gz -> {
                      try {
                          return getPricesFromCompressedArchive(gz).stream();
                      } catch (IOException e) {
                          throw new UncheckedIOException(e);
                      }
                  });
        } catch (UncheckedIOException exception) {
            throw new IOException(exception);
        }
    }

    public static Stream<IStockTicker> getPystockStockPriceReader(LocalDate startDate, LocalDate endDate) throws IOException {
        Set<LocalDate> desirableDates = getDateRange(startDate, endDate.plusDays(1));
        return PystockDataReader.read(f -> fileNameMatchesDate(f, desirableDates));
    }

    public static Stream<IStockTicker> fromDate(LocalDate date) throws IOException {
        return getPystockStockPriceReader(date, date);
    }

    @SuppressWarnings("PMD.AssignmentInOperand")
    private static List<IStockTicker> getPricesFromCompressedArchive(InputStream compressedArchive) throws IOException {
        TarArchiveInputStream stream = new TarArchiveInputStream(new GzipCompressorInputStream(compressedArchive));
        TarArchiveEntry entry;
        while ((entry = stream.getNextTarEntry()) != null) {
            if ("prices.csv".equals(entry.getName())) {
                try (BufferedReader reader = new BufferedReader(new InputStreamReader(stream, StandardCharsets.UTF_8))) {
                    // Skip header. Stream has to be collected so that it lives outside of the try-with-resources block
                    return reader.lines().skip(1).map(PystockDataReader::parseStockPrice).filter(Optional::isPresent).map(Optional::get).collect(Collectors.toList());
                }
            }
        }

        return Collections.emptyList();
    }

    private static Optional<IStockTicker> parseStockPrice(String line) {
        if (line.startsWith("symbol")) {
            return Optional.empty();
        }
        String[] tokens = line.split(",");
        return Optional.of(new StockTicker(tokens[0], Double.valueOf(tokens[3]), Double.valueOf(tokens[4]), Double.valueOf(tokens[5]), Long.parseLong(tokens[6]), parseTimestamp(tokens[1])));
    }

    private static LocalDateTime parseTimestamp(String line) {
        String[] tokens = line.split("-");
        return LocalDateTime.of(Integer.parseInt(tokens[0]), Integer.parseInt(tokens[1]), Integer.parseInt(tokens[2]), 0, 0, 0, 0);
    }
}
