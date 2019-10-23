package qwde.dataprovider.models;

import qwde.dataprovider.util.DateUtil;

import java.time.LocalDateTime;

public class StockTicker implements IStockTicker {
    public final String symbol;
    public final double closePrice;
    public final double highPrice;
    public final double lowPrice;
    public final Long volume;
    public final LocalDateTime timestamp;

    public StockTicker(String symbol, double price, double highPrice, double lowPrice, Long volume, LocalDateTime timestamp) {
        this.symbol = symbol;
        this.closePrice = price;
        this.highPrice = highPrice;
        this.lowPrice = lowPrice;
        this.volume = volume;
        this.timestamp = timestamp;
    }

    @Override
    public double getPrice() {
        return this.closePrice;
    }

    @Override
    public double getHigh() {
        return this.highPrice;
    }

    @Override
    public double getLow() {
        return this.lowPrice;
    }

    @Override
    public Long getVolume() {
        return this.volume;
    }

    @Override
    public String getCompany() {
        return this.symbol;
    }

    @Override
    public LocalDateTime getTimestamp() {
        return this.timestamp;
    }

    @Override
    public int compareTo(IStockTicker other) {
        if (this.symbol.equals(other.getCompany())) {
            return DateUtil.compareDdMmYyyy(this.timestamp, other.getTimestamp());
        } else {
            return this.symbol.compareTo(other.getCompany());
        }
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof IStockTicker) {
            return compareTo((IStockTicker) other) == 0;
        } else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
