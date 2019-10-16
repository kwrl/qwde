package main.qwde.trading.engine;

import main.qwde.trading.realtime.algorithm.BuyLowSellHigh;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import qwde.dataprovider.models.StockTicker;
import com.google.common.collect.TreeMultiSet;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class TradeEngine {
  private static TradeEngine engine;
  private List<BuyLowSellHigh> algorithms = new ArrayList<>();

  public static TradeEngine getInstance() {
    if (engine == null) {
      engine = new TradeEngine();
    }
    return engine;
  }

  public Runnable pollData(KafkaConsumer<String, StockTicker> consumer) {
    final int giveUp = 100;
    int noRecordsCount = 0;

    while (true) {
      final ConsumerRecords<String, StockTicker> consumerRecords = consumer.poll(Duration.ofSeconds(1));

      if (consumerRecords.count()==0) {
        noRecordsCount++;
        if (noRecordsCount > giveUp) break;
        else continue;
      }

      for (BuyLowSellHigh algo : this.algorithms) {
        for (ConsumerRecord<String, StockTicker> record : consumerRecords.records("stocktickers")) {
          // for all pending market orders, issue a trade at current price + whatever
          //
          // for all pending regular orders, check if price <= or >=, then issue a trade at price + commission
          algo.processTick(record);
        }
      }

      consumer.commitAsync();

    }

    consumer.close();

    System.out.println("DONE");

  }

  public void addTradingAlgorithm(BuyLowSellHigh algorithm) {
    this.algorithms.add(algorithm);
  }

  private final List<Order> buyOrders = new ArrayList<>();
  private final List<Order> sellOrders = new ArrayList<>();
  private final List<MarketOrder> marketBuyOrders = new ArrayList<>();
  private final List<MarketOrder> marketSellOrders = new ArrayList<>();
  private final List<Trade> trades = new ArrayList<>();
  private final List<Order> pendingBuyOrders = new TreeMultiSet<>();

  public MarketOrder placeBidMarketOrder(String ticker, double price, long quantity, LocalDateTime time) {
    this.marketBuyOrders.add(new Order(time, ticker, price, quantity, TradeType.BID));

    // Pretend we got a matching trade at current price + a spread between .05 and 2 percent
    double tradePrice = price + (price * (0.05 + (new Random().nextDouble() % 0.15)));
    Order trade = new Order(time, ticker, tradePrice, quantity, TradeType.TRADE);
    this.trades.add(trade);

    return trade;
  }

  // TODO: market orders are given right away, limit orders are continously checked (in next batch of data)
  public MarketOrder placeAskMarketOrder(String ticker, double price, long quantity, LocalDateTime time) {
    this.marketSellOrders.add(new Order(time, ticker, price, quantity, TradeType.ASK));

    // Pretend we got a matching trade at current price - a spread between .05 and 2 percent
    Order trade = new Order(time, ticker, tradePrice, quantity, TradeType.TRADE);
    this.trades.add(trade);

    return trade;
  }
}
