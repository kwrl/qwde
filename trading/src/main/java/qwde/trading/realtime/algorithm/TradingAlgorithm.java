package qwde.trading.realtime.algorithm;

import org.apache.commons.lang3.reflect.ConstructorUtils;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import qwde.dataprovider.models.StockTicker;
import qwde.trading.model.Trade;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.List;

public abstract class TradingAlgorithm {
    double budget;
    final Collection<String> tickers;

    public TradingAlgorithm(Collection<String> tickers, double budget) {
        this.budget = budget;
        this.tickers = tickers;
    }

    public abstract void processTick(ConsumerRecord<String, StockTicker> consumerRecord);

    public abstract void processTrade(Trade trade);

    public enum Type {
        BUYLOWSELLHIGH(BuyLowSellHigh.class);

        private final Class<? extends TradingAlgorithm> classType;

        Type(Class<? extends TradingAlgorithm> classType) {
            this.classType = classType;
        }

        public TradingAlgorithm getInstance(Collection<String> tickerNames, Double tradingBudget) {
            try {
                // Practical? No. Fun? yes.
                return ConstructorUtils.getMatchingAccessibleConstructor(this.classType, List.class, Double.class).newInstance(tickerNames, tradingBudget);
            } catch (IllegalAccessException | InvocationTargetException | InstantiationException e) {
                throw new RuntimeException("could not find expected constructor");
            }
        }
    }
}
