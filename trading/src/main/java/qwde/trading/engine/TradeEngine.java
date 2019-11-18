package qwde.trading.engine;

import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.dataprovider.kafka.db.SqlliteKafkaStore;
import qwde.dataprovider.models.StockTicker;
import qwde.trading.model.MarketOrder;
import qwde.trading.model.Order;
import qwde.trading.model.Summary;
import qwde.trading.model.Trade;
import qwde.trading.realtime.algorithm.TradingAlgorithm;

import java.io.IOException;
import java.sql.SQLException;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class TradeEngine {
    private static final Logger LOG = LoggerFactory.getLogger(TradeEngine.class);
    private final List<TradingAlgorithm> algorithms = new ArrayList<>();
    private int tickCounter;
    private final List<Order> buyOrders = new ArrayList<>();
    private final List<Order> sellOrders = new ArrayList<>();
    private final List<MarketOrder> buyHistory = new ArrayList<>();
    private final List<Trade> trades = new ArrayList<>();
    private final List<Order> pendingBuyOrders = new ArrayList<>();
    private final List<Order> pendingAskOrders = new ArrayList<>();
    private final Stack<MarketOrder> pendingMarketOrders = new Stack<>();

    private static class LazyHolder {
        static final TradeEngine INSTANCE = new TradeEngine();
    }

    public static TradeEngine getInstance() {
        return LazyHolder.INSTANCE;
    }

    public Summary pollDataSql(List<String> tickers, LocalDate fromDate, LocalDate endDate) throws IOException {
        try (KafkaConsumer<String, StockTicker> kafkaConsumer = SqlliteKafkaStore.sqlliteKafkaStore(tickers, fromDate, endDate)) {
            return pollData(kafkaConsumer, SqlliteKafkaStore.TOPIC);
        } catch (SQLException exception) {
            throw new IOException(exception);
        }
    }

    public Summary pollData(KafkaConsumer<String, StockTicker> consumer, String topic) {
        if (this.algorithms.isEmpty()) {
            throw new IllegalStateException("No trading algorithms to process");
        }
        final int giveUp = 3;
        int noRecordsCount = 0;

        while (true) {
            final ConsumerRecords<String, StockTicker> consumerRecords = consumer.poll(Duration.ofSeconds(1));

            if (consumerRecords.count() == 0) {
                LOG.info("No records polled, attempt {}/{}", noRecordsCount, giveUp);
                noRecordsCount++;
                if (noRecordsCount > giveUp) {
                    break;
                } else {
                    continue;
                }
            }

            for (TradingAlgorithm algo : this.algorithms) {
                for (ConsumerRecord<String, StockTicker> record : consumerRecords.records(topic)) {
                    // for all pending market orders, issue a trade at current price + whatever
                    //
                    // for all pending regular orders, check if price <= or >=, then issue a trade at price + commission
                    while (!this.pendingMarketOrders.isEmpty()) {
                        double tradePrice = record.value().closePrice;
                        MarketOrder order = this.pendingMarketOrders.pop();
                        Trade trade = new Trade(LocalDateTime.now(), order.ticker, tradePrice, order.quantity, order.isBid);
                        this.trades.add(trade);
                        this.buyHistory.add(order);
                        algo.processTrade(trade);
                        LOG.debug("Placed trade from market order, price {}, quantity {} @ {}", trade.price, trade.quantity, trade.timeOfOrder);
                    }

                    algo.processTick(record);
                    tickCounter += 1;
                    LOG.trace("Processed tick #{}", tickCounter);
                }
            }

            consumer.commitAsync();
        }

        consumer.close();

        return new Summary(this.buyOrders, this.sellOrders, this.buyHistory, this.trades, this.pendingBuyOrders, this.pendingAskOrders);

    }

    public void addTradingAlgorithm(TradingAlgorithm algorithm) {
        this.algorithms.add(algorithm);
    }

    public void placeBidMarketOrder(String ticker, long quantity) {
        LocalDateTime timestamp = LocalDateTime.now();
        this.pendingMarketOrders.add(new MarketOrder(timestamp, ticker, quantity, true));
        LOG.info("Market BID order placed");
    }

    public void placeAskMarketOrder(String ticker, long quantity) {
        LocalDateTime timestamp = LocalDateTime.now();
        this.pendingMarketOrders.add(new MarketOrder(timestamp, ticker, quantity, false));
        LOG.info("Market ASK order placed");
    }
}
