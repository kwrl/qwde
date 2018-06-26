package qwde.models;

import java.time.LocalDateTime;

public interface StockPrice {
	public double getPrice();
	public String getCompany();
	public LocalDateTime getTimestamp();
}