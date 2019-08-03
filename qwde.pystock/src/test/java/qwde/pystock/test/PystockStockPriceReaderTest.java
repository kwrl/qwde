package qwde.pystock.test;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

import org.junit.jupiter.api.Test;

import com.google.common.truth.Truth;

import qwde.pystock.PystockStockPriceReader;

public class PystockStockPriceReaderTest {
  private PystockStockPriceReader getPystockPriceReader(LocalDate date) throws IOException {
    ClassLoader classLoader = PystockStockPriceReaderTest.class.getClassLoader();
    InputStream resourceStream = classLoader.getResourceAsStream(date.format(PystockStockPriceReader.dateTimeFormatter) + ".tar.gz");
    if (resourceStream == null) {
      throw new FileNotFoundException(String.format("Could not find file '%s'", date));
    }

    return new PystockStockPriceReader(resourceStream);
  }

  @Test
  public void readSpecificTickerInfo() throws IOException {
    PystockStockPriceReader pystockStockPriceReader = new PystockStockPriceReader(LocalDate.of(2017, 01, 02), LocalDate.of(2017, 01, 02));

    BigDecimal twitterPrice = pystockStockPriceReader.read().stream().filter(x -> x.getCompany().equals("TWTR")).map(x -> x.getPrice()).findFirst().get();
    
    Truth.assertThat(twitterPrice).isEqualToIgnoringScale("16.3949995");
  }

  @Test
  public void testRead() throws IOException {
    PystockStockPriceReader pyReader = getPystockPriceReader(LocalDate.of(2017, 01, 02));

    Truth.assertThat(pyReader.read().isEmpty()).isFalse();
    Truth.assertThat(pyReader.read().get(0).getPrice()).isEqualToIgnoringScale("4.655");
    Truth.assertThat(pyReader.read().get(0).getCompany()).isEqualTo("FAX");
    Truth.assertThat(pyReader.read().get(0).getTimestamp()).isEqualTo(LocalDateTime.of(2016, 12, 30, 0, 0));
  }
}
