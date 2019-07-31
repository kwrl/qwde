package qwde.pystock;

import java.io.IOException;
import java.time.LocalDate;

public class PystockSelection {
    public PystockSelection(String ticker, LocalDate fromDate, LocalDate toDate) throws IOException {
        PystockStockPriceReader pystockStockPriceReader = PystockStockPriceReader.FromDate(fromDate);
    }
}
