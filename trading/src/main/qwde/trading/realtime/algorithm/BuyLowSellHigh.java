package main.qwde.trading.realtime.algorithm;

import main.qwde.trading.engine.TradeEngine;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import qwde.dataprovider.models.StockTicker;
import qwde.web.servlets.StockStatistics;

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

  private final List<BigDecimal> pricesSeen = new ArrayList<>();

  private final String ticker;
  private double budget;

  public BuyLowSellHigh(StockTicker ticker, double budget) {
    this.ticker = ticker;
    this.budget = budget;
  }

  public void processTick(ConsumerRecord<String, StockTicker> record) {
    StockTicker stockTicker = record.value();
    pricesSeen.add(stockTicker.getPrice());

    if (lowestPriceSoFar.compareTo(stockTicker.getPrice()) < 0) {
      lowestPriceSoFar = stockTicker.getPrice();
      Order trade = TradeEngine.getInstance().placeBidMarketOrder(lowestPriceSoFar);
      this.budget -= trade.price * trade.quantity;
    }
     // If order is pending, do nothing
    // if price is 20% higher than 
  }

  public void processTrade(Trade trade) {
    if (trade.isBid) {
      this.budget -= trade.getPrice();
    } else {
      this.budget += trade.getPrice();
    }
  }
}
