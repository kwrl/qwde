package qwde.pystock;

import java.sql.Timestamp;

import qwde.models.StockPrice;
import qwde.ml.LinearRegression;

public class PystockStockPrice implements StockPrice {
	private final double highPrice;
	private final double lowPrice;
	private final String company;
	private final Timestamp timestamp;
	LinearRegression test;

	public PystockStockPrice(double highPrice, double lowPrice, String company, Timestamp timestamp) {
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
	public Timestamp getTimestamp() {
		return timestamp;
	}
	
	@SuppressWarnings("deprecation")
	@Override
	public String toString() {
		return company + " [" + timestamp.toGMTString() +"]: "+getPrice();
	}

}
