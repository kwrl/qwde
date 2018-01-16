package qwde.models;

import java.sql.Timestamp;

public interface StockPrice {
	public double getPrice();
	public String getCompany();
	public Timestamp getTimestamp();
}
