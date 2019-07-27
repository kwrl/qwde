package qwde.pystock.test;

import java.io.File;
import java.io.IOException;
import java.time.LocalDateTime;

import org.junit.Test;

import com.google.common.truth.Truth;

import qwde.pystock.PystockStockPriceReader;

public class PystockStockPriceReaderTest {
  @Test
  public void testRead() throws IOException {
    ClassLoader classLoader = getClass().getClassLoader();
    File pyStockFile = new File(classLoader.getResource("20170102.tar.gz").getFile());

    PystockStockPriceReader pyReader = new PystockStockPriceReader(pyStockFile);

    Truth.assertThat(pyReader.read().isEmpty()).isFalse();
    Truth.assertThat(pyReader.read().get(0).getPrice()).isEqualTo(4.655);
    Truth.assertThat(pyReader.read().get(0).getCompany()).isEqualTo("FAX");
    Truth.assertThat(pyReader.read().get(0).getTimestamp()).isEqualTo(LocalDateTime.of(2016, 12, 30, 0, 0));
  }
}
