package qwde.pystock;

import java.time.LocalDateTime;

import qwde.models.StockPrice;
import qwde.ml.LinearRegression;

public class PystockStockPrice implements StockPrice {
	private final double highPrice;
	private final double lowPrice;
	private final String company;
	private final LocalDateTime timestamp;
	LinearRegression test;

	public PystockStockPrice(double highPrice, double lowPrice, String company, LocalDateTime timestamp) {
		super();
		this.highPrice = highPrice;
		this.lowPrice = lowPrice;
		this.company = company;
		this.timestamp = timestamp;
	}

	public double getHighPrice() {
		return highPrice;
	}

	public double getLowPrice() {
		return lowPrice;
	}

	@Override
	public double getPrice() {
		return (highPrice+lowPrice)/2.0;
	}

	@Override
	public String getCompany() {
		return company;
	}

	@Override
	public LocalDateTime getTimestamp() {
		return timestamp;
	}
	
	@Override
	public String toString() {
		return company + " [" + timestamp.toString() +"]: " + getPrice();
	}

}
