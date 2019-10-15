package main.qwde.trading.engine;

import main.qwde.trading.realtime.algorithm.BuyLowSellHigh;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import qwde.dataprovider.models.StockTicker;

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
        final int giveUp = 100;   int noRecordsCount = 0;

        while (true) {
            final ConsumerRecords<String, StockTicker> consumerRecords = consumer.poll(Duration.ofSeconds(1));

            if (consumerRecords.count()==0) {
                noRecordsCount++;
                if (noRecordsCount > giveUp) break;
                else continue;
            }

            this.algorithms.stream().forEach(a -> a.decide(consumerRecords.records("stocktickers")));

            consumer.commitAsync();

        }

        consumer.close();

        System.out.println("DONE");

    }

    public void addTradingAlgorithm(BuyLowSellHigh algorithm) {
        this.algorithms.add(algorithm);
    }

    public void placeBidMarketOrder(String ticker, double amount) {
    }

    public void placeBid(String ticker, double amount) {
    }

    public void placeAsk(String ticker, double amount) {
    }
}
