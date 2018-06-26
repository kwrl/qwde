package qwde.util;

import java.util.Collection;

import qwde.models.StockPrice;

public interface StockPriceReader {
	public Collection<StockPrice> read();
}
