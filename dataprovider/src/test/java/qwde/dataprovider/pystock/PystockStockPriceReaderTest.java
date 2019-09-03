package qwde.dataprovider.pystock;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import com.google.common.truth.Truth;
import qwde.dataprovider.models.StockPrice;

class PystockStockPriceReaderTest {

  @Test
  void readSpecificTickerInfo() throws IOException {
    PystockStockPriceReader pystockStockPriceReader = PystockStockPriceReader.getPystockStockPriceReader(LocalDate.of(2017, 1, 2), LocalDate.of(2017, 1, 2));


    Optional<BigDecimal> twitterPrice = pystockStockPriceReader.read().stream().filter(x -> x.getCompany().equals("TWTR")).map(StockPrice::getPrice).findFirst();

    Truth.assertThat(twitterPrice.isPresent()).isTrue();
    Truth.assertThat(twitterPrice.get()).isEqualToIgnoringScale("16.299999");
  }

  @Test
  void readSpecificTickerInfoRange() throws IOException {
    PystockStockPriceReader pystockStockPriceReader = PystockStockPriceReader.getPystockStockPriceReader(LocalDate.of(2017, 1, 2), LocalDate.of(2017, 1, 20));
  
    List<BigDecimal> twitterPrice = pystockStockPriceReader.read().stream().filter(x -> x.getCompany().equals("TWTR")).map(StockPrice::getPrice).collect(Collectors.toList());
  
    // Interval is 20 - 2 days, and since its an inclusive range, we add 1...
    // Minus number of weekends...
    // 16th of January is a holiday...
    Truth.assertThat(twitterPrice.size()).isEqualTo((20 - 2 + 1) - (2 * 2) - 1);
  }
  
  @Test
  void testRead() throws IOException {
    PystockStockPriceReader pyReader = PystockStockPriceReader.getPystockStockPriceReader(LocalDate.of(2017, 1, 2), LocalDate.of(2017, 1, 2));
  
    Truth.assertThat(pyReader.read().isEmpty()).isFalse();
    Truth.assertThat(pyReader.read().get(0).getPrice()).isEqualToIgnoringScale("45.560001");
    Truth.assertThat(pyReader.read().get(0).getCompany()).isEqualTo("A");
    Truth.assertThat(pyReader.read().get(0).getTimestamp()).isEqualTo(LocalDateTime.of(2016, 12, 30, 0, 0));
  }
}
