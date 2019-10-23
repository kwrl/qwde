package qwde.trading.model;

import java.time.LocalDateTime;

public class MarketOrder {
    public final LocalDateTime timeOfOrder;
    public final String ticker;
    public final long quantity;
    public final boolean isBid;

    public MarketOrder(LocalDateTime timeOfOrder, String ticker, long quantity, boolean isBid) {
        this.timeOfOrder = timeOfOrder;
        this.ticker = ticker;
        this.quantity = quantity;
        this.isBid = isBid;
    }
}
