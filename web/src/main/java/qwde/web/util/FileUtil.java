package qwde.web.util;

import java.io.IOException;
import java.net.URL;

import com.google.common.base.Charsets;
import com.google.common.io.Resources;

public final class FileUtil {
  private FileUtil() {
  }

  public static String getResourceFile(String filename) throws IOException {
    URL url = Resources.getResource(filename);
    return Resources.toString(url, Charsets.UTF_8);
  }
}
