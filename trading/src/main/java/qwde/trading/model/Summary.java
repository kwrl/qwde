package qwde.trading.model;

import com.google.common.collect.ImmutableList;

import java.util.List;

public final class Summary {
    public final List<Order> buyOrders;
    public final List<Order> sellOrders;
    public final List<MarketOrder> buyHistory;
    public final List<Trade> trades;
    public final List<Order> pendingBuyOrders;
    public final List<Order> pendingAskOrders;

    public Summary(List<Order> buyOrders, List<Order> sellOrders, List<MarketOrder> buyHistory, List<Trade> trades, List<Order> pendingBuyOrders, List<Order> pendingAskOrders) {
        this.buyOrders = ImmutableList.copyOf(buyOrders);
        this.sellOrders = ImmutableList.copyOf(sellOrders);
        this.buyHistory = ImmutableList.copyOf(buyHistory);
        this.trades = ImmutableList.copyOf(trades);
        this.pendingBuyOrders = ImmutableList.copyOf(pendingBuyOrders);
        this.pendingAskOrders = ImmutableList.copyOf(pendingAskOrders);
    }
}

