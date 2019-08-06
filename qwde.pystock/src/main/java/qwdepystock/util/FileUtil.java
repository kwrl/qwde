package qwdepystock.util; 

import java.util.stream.Collectors;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;

public class FileUtil {
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
}
