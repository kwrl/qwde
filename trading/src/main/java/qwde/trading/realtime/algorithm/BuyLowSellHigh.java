package qwde.trading.realtime.algorithm;

import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.dataprovider.models.StockTicker;
import qwde.trading.engine.TradeEngine;
import qwde.trading.model.Trade;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class BuyLowSellHigh extends TradingAlgorithm {
    private static final Logger LOG = LoggerFactory.getLogger(BuyLowSellHigh.class);
    private Double lowestPriceSoFar = Double.MAX_VALUE;

    private final List<Double> pricesSeen = new ArrayList<>();

    public BuyLowSellHigh(Collection<String> tickers, double budget) {
        super(tickers, budget);
    }

    @Override
    public void processTick(ConsumerRecord<String, StockTicker> record) {
        StockTicker stockTicker = record.value();
        pricesSeen.add(stockTicker.getPrice());

        if (lowestPriceSoFar > stockTicker.getPrice()) {
            lowestPriceSoFar = stockTicker.getPrice();
            if (super.budget > lowestPriceSoFar.doubleValue() * 100L) {
                TradeEngine.getInstance().placeBidMarketOrder(record.key(), 100L);
            } else {
                LOG.info("Not making trade for price {} since budget is too low ({})", lowestPriceSoFar, super.budget);
            }
        }
        // If order is pending, do nothing
        // if price is 20% higher than
    }

    @Override
    public void processTrade(Trade trade) {
        if (trade.isBid) {
            this.budget -= trade.getTotalPrice();
        } else {
            this.budget += trade.getTotalPrice();
        }
    }
}
