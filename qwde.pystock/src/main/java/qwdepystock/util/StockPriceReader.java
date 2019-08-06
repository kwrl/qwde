package qwdepystock.util;

import java.util.Collection;

import qwdepystock.models.StockPrice;

public interface StockPriceReader {
  Collection<StockPrice> read();
}
