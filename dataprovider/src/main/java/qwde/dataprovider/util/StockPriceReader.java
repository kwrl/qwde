package qwde.dataprovider.util;

import java.util.Collection;

import qwde.dataprovider.models.StockPrice;

public interface StockPriceReader {
  Collection<StockPrice> read();
}
