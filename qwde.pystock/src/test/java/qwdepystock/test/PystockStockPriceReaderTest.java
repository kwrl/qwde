package qwdepystock.test;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.time.LocalDateTime;

import org.junit.jupiter.api.Test;

import com.google.common.truth.Truth;

import qwdepystock.pystock.PystockStockPrice;
import qwdepystock.pystock.PystockStockPriceReader;

public class PystockStockPriceReaderTest {
  // @Test
  public void readSpecificTickerInfo() throws IOException {
    PystockStockPriceReader pystockStockPriceReader = PystockStockPriceReader.getPystockStockPriceReader(LocalDate.of(2017, 01, 02), LocalDate.of(2017, 01, 02));

    BigDecimal twitterPrice = pystockStockPriceReader.read().stream().filter(x -> x.getCompany().equals("TWTR")).map(x -> x.getPrice()).findFirst().get();
    
    Truth.assertThat(twitterPrice).isEqualToIgnoringScale("16.299999");
  }

  @Test
  public void readSpecificTickerInfo_Range() throws IOException {
    PystockStockPriceReader pystockStockPriceReader = PystockStockPriceReader.getPystockStockPriceReader(LocalDate.of(2017, 1, 2), LocalDate.of(2017, 1, 20));
    
    List<BigDecimal> twitterPrice = pystockStockPriceReader.read().stream().filter(x -> x.getCompany().equals("TWTR")).map(x -> x.getPrice()).collect(Collectors.toList());

    // Interval is 20 - 2 days, and since its an inclusive range, we add 1...
    // Minus number of weekends...
    // 16th of January is a holiday...
    Truth.assertThat(twitterPrice.size()).isEqualTo((20-2+1)-(2*2) - 1);
  }

  // @Test
  public void testRead() throws IOException {
    PystockStockPriceReader pyReader = PystockStockPriceReader.getPystockStockPriceReader(LocalDate.of(2017, 01, 02), LocalDate.of(2017, 01, 02));

    Truth.assertThat(pyReader.read().isEmpty()).isFalse();
    Truth.assertThat(pyReader.read().get(0).getPrice()).isEqualToIgnoringScale("4.63");
    Truth.assertThat(pyReader.read().get(0).getCompany()).isEqualTo("FAX");
    Truth.assertThat(pyReader.read().get(0).getTimestamp()).isEqualTo(LocalDateTime.of(2016, 12, 30, 0, 0));
  }
}
