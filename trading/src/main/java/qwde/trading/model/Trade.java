package qwde.trading.model;

import java.time.LocalDateTime;

public class Trade extends Order {
    public Trade(LocalDateTime timeOfOrder, String ticker, double price, long quantity, boolean isBid) {
        super(timeOfOrder, ticker, price, quantity, isBid);
    }
}
