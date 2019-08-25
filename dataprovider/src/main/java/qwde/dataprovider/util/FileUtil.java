package qwde.dataprovider.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.NotImplementedException;
import org.apache.commons.lang3.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class FileUtil {
  private static Logger logger = LoggerFactory.getLogger(FileUtil.class);
  public static final String APPNAME = "qwde";

  private FileUtil() {
  }

  public static String getResourceFileAsString(String fileName) throws FileNotFoundException, IOException {
    InputStream is = FileUtil.class.getClassLoader().getResourceAsStream(fileName);
    if (is == null) {
      throw new FileNotFoundException("Could not find " + fileName);
    }
    BufferedReader reader = new BufferedReader(new InputStreamReader(is));
    return reader.lines().collect(Collectors.joining(System.lineSeparator()));
  }

  public static String createIfNotExists(String path) {
    File pathAsFile = new File(path);
    if (!pathAsFile.exists()) {
      pathAsFile.mkdirs();
    }

    return path;
  }

  public static String getCacheDirectory() {
    if (SystemUtils.IS_OS_WINDOWS) {
      return Path.of(System.getenv("APPDATA"), APPNAME, "cache").toAbsolutePath().toString();
    } else if (SystemUtils.IS_OS_UNIX) {
      String xdgDataHome = System.getenv("XDG_CACHE_HOME");
      if (xdgDataHome == null || xdgDataHome.isEmpty()) {
        xdgDataHome = Path.of(System.getenv("HOME"), ".cache", APPNAME).toAbsolutePath().toString();
      }
      return Path.of(xdgDataHome, APPNAME).toAbsolutePath().toString();
    } else {
      throw new NotImplementedException("Only defined data dirs for windows and unix-like (XDG) OS, so far");
    }
  }

  public static Optional<Path> findInPath(String target, String path) {
    if (path.isEmpty()) {
      return Optional.empty();
    }
    String expandedPath = path.replaceFirst("^~", Matcher.quoteReplacement(System.getProperty("user.home")));

    File dir = new File(expandedPath);
    if (!dir.exists()) {
      return Optional.empty();
    }

    File[] files = dir.listFiles((d, name) -> name.equals(target));
    if (files.length > 0) {
      Path destPath = files[0].toPath();
      if (Files.isSymbolicLink(destPath)) {
        try {
          Path p = Files.readSymbolicLink(destPath);
          if (p.toFile().exists()) {
            return Optional.of(p);
          }
        } catch (IOException exception) {
          logger.warn("Tried to read {} as symbolic link, target not found", target);
          return Optional.empty();
        }
      }
      return Optional.of(destPath);
    }

    return Optional.empty();
  }

  public static Optional<Path> findFolderInDatapath(String folder) {
    if (SystemUtils.IS_OS_WINDOWS) {
      return findInPath(Path.of(System.getenv("APPDATA"), APPNAME, "data").toAbsolutePath().toString(), folder);
    } else if (SystemUtils.IS_OS_UNIX) {
      String xdgDataHome = System.getenv("XDG_DATA_HOME");
      if (xdgDataHome == null || xdgDataHome.isEmpty()) {
        xdgDataHome = "/usr/local/share:/usr/share";
      }
      String xdgDataDirs = System.getenv("XDG_DATA_DIRS");
      if (xdgDataHome == null || xdgDataHome.isEmpty()) {
        xdgDataDirs = "";
      }

      logger.debug("XDG_DATA_HOME: {}", xdgDataHome);
      logger.debug("XDG_DATA_DIRS: {}", xdgDataDirs);

      String xdgPaths = String.format("%s:%s", xdgDataHome, xdgDataDirs);
      return Stream.of(xdgPaths.split(":")).map(s -> findInPath(folder, s)).filter(Optional::isPresent).map(Optional::get).findFirst();
    } else {
      throw new NotImplementedException("Only defined data dirs for windows and unix-like (XDG) OS, so far");
    }
  }
}
