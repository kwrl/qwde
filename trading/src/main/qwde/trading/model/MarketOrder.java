package qwde.trading.model;

public class MarketOrder {
  public final LocalDateTime timeOfOrder;
  public final String ticker;
  public final long quantity;
  public final boolean isBid;

  public Order(LocalDateTime timeOfOrder, String ticker, long quantity, boolean isBid) {
    this.timeOfOrder = timeOfOrder;
    this.ticker = ticker;
    this.quantity = quantity;
    this.isBid = isBid;
  }
  
  public double getTotalPrice() {
    return price * quantity;
  }
}


