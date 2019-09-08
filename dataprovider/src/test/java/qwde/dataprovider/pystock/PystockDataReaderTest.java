package qwde.dataprovider.pystock;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import com.google.common.truth.Truth;
import qwde.dataprovider.models.IStockTicker;

class PystockDataReaderTest {

  @Test
  void readSpecificTickerInfo() throws IOException {
    Stream<IStockTicker> stockData = PystockDataReader.getPystockStockPriceReader(LocalDate.of(2017, 1, 2), LocalDate.of(2017, 1, 2));

    Optional<BigDecimal> twitterPrice = stockData.filter(x -> x.getCompany().equals("TWTR")).map(IStockTicker::getPrice).findFirst();

    Truth.assertThat(twitterPrice.isPresent()).isTrue();
    Truth.assertThat(twitterPrice.get()).isEqualToIgnoringScale("16.57");
  }

  @Test
  void readSpecificTickerInfoRange() throws IOException {
    Stream<IStockTicker> stockData = PystockDataReader.getPystockStockPriceReader(LocalDate.of(2017, 1, 2), LocalDate.of(2017, 1, 20));

    List<BigDecimal> twitterPrice = stockData.filter(x -> x.getCompany().equals("TWTR")).map(IStockTicker::getPrice).sorted().distinct().collect(Collectors.toList());
  
    // Interval is 20 - 2 days, and since its an inclusive range, we add 1...
    // Minus number of weekends...
    // 16th of January is a holiday...
    Truth.assertThat(twitterPrice.size()).isEqualTo((20 - 2 + 1) - (2 * 2) - 1);
  }
}
