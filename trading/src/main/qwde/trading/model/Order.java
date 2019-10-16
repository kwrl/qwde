package qwde.trading.model;

public class Order {
  public final LocalDateTime timeOfOrder;
  public final String ticker;
  public final double price;
  public final long quantity;
  public final TradeType tradeType;

  public Order(LocalDateTime timeOfOrder, String ticker, double price, long quantity, TradeType type) {
    this.timeOfOrder = timeOfOrder;
    this.ticker = ticker;
    this.price = price;
    this.quantity = quantity;
    this.tradeType = tradeType;
  }
}

