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
    private BigDecimal lowerstPriceSoFar = new BigDecimal(999999);
    private BigDecimal highestPriceSoFar = BigDecimal.ZERO;

    Map<String, List<LocalTime>> bidsPlaced = new HashMap<>();
    Map<String, List<LocalTime>> asksPlaced = new HashMap<>();

    public BuyLowSellHigh(List<StockTicker> tickers) {
        tickers.stream().forEach(s -> {
            bidsPlaced.put(s, new ArrayList<>());
            asksPlaced.put(s, new ArrayList<>());
        });
    }

    public void decide(Iterable<ConsumerRecord<String, StockTicker>> stockTickers) {
        List<BigDecimal> minPrice = StreamSupport.stream(stockTickers.spliterator(), false).map(s -> s.value()).map(StockTicker::getPrice).min(Comparator.naturalOrder()).get();

        if (lowerstPriceSoFar.compareTo(minPrice) < 0) {
            TradeEngine.getInstance().placeBidMarketOrder(lowerstPriceSoFar);
        }
    }
}
