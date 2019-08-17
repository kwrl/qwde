package qwdepystock.util; 

import java.util.stream.Collectors;
import java.io.BufferedReader;
import java.nio.file.Path;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;

import org.apache.commons.lang3.NotImplementedException;
import org.apache.commons.lang3.SystemUtils;

public class FileUtil {
  public static String APPNAME = "qwde";

  public static String getResourceFileAsString(String fileName) throws FileNotFoundException, IOException {
    InputStream is = FileUtil.class.getClassLoader().getResourceAsStream(fileName);
      if (is == null) {
        throw new FileNotFoundException("Could not find " + fileName);
      }
      BufferedReader reader = new BufferedReader(new InputStreamReader(is));
      return reader.lines().collect(Collectors.joining(System.lineSeparator()));
  }

  public static String getGitRootDirectory() {
    ProcessBuilder builder = new ProcessBuilder("git", "rev-parse", "--show-toplevel");
    builder.redirectErrorStream(true);
    try {
      Process process = builder.start();
      InputStream is = process.getInputStream();
      BufferedReader reader = new BufferedReader(new InputStreamReader(is));

      return reader.readLine();
    } catch (IOException exception) {
      throw new UncheckedIOException(exception);
    }
  }
  
  public static String getApplicationDataDirectory() {
    if (SystemUtils.IS_OS_WINDOWS) {
      return Path.of(System.getenv("APPDATA"), APPNAME).toAbsolutePath().toString();
    } else if (SystemUtils.IS_OS_UNIX) {
      String xdgDataHome = System.getenv("XDG_DATA_HOME");
      if (xdgDataHome.isEmpty()) {
        xdgDataHome = Path.of(System.getenv("HOME"), ".cache", APPNAME).toAbsolutePath().toString();
      } 
      return Path.of(xdgDataHome, APPNAME).toAbsolutePath().toString();
    } else {
      throw new NotImplementedException("Only defined data dirs for windows and unix-like (XDG) OS, so far");
    }
  }
}
