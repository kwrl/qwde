package main.qwde.trading.realtime.algorithm;

import main.qwde.trading.engine.TradeEngine;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import qwde.dataprovider.models.StockTicker;

import java.math.BigDecimal;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class BuyLowSellHigh {
    private BigDecimal lowestPriceSoFar = new BigDecimal(999999);
    private BigDecimal highestPriceSoFar = BigDecimal.ZERO;

    private final String ticker;
    private double budget;

    public BuyLowSellHigh(StockTicker ticker, double budget) {
      this.ticker = ticker;
      this.budget = budget;
    }

    public void decide(Iterable<ConsumerRecord<String, StockTicker>> stockTickers) {
        List<BigDecimal> minPrice = StreamSupport.stream(stockTickers.spliterator(), false).map(s -> s.value()).map(StockTicker::getPrice).min(Comparator.naturalOrder()).get();

        if (lowestPriceSoFar.compareTo(minPrice) < 0) {
            Order trade = TradeEngine.getInstance().placeBidMarketOrder(lowestPriceSoFar);
            this.budget -= trade.price * trade.quantity;
        }
    }
}
