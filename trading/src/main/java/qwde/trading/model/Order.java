package qwde.trading.model;

import java.time.LocalDateTime;

public class Order {
    public final LocalDateTime timeOfOrder;
    public final String ticker;
    public final double price;
    public final long quantity;
    public final boolean isBid;

    public Order(LocalDateTime timeOfOrder, String ticker, double price, long quantity, boolean isBid) {
        this.timeOfOrder = timeOfOrder;
        this.ticker = ticker;
        this.price = price;
        this.quantity = quantity;
        this.isBid = isBid;
    }

    public double getTotalPrice() {
        return price * quantity;
    }
}

