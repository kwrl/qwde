package qwde.pystock;

import java.io.File;

import qwde.models.StockPrice;
import qwde.util.StockPriceReader;

public class PystockStockPriceReader implements StockPriceReader {
	public PystockStockPriceReader(String directoryPath) {
		File directory = new File(directoryPath);
		for(File file : directory.listFiles()){
			System.out.println(file.getName());
		}
	}

	public StockPrice read() {
		return null;
	}

	public static void main(String[] args) {
		new PystockStockPriceReader("C:\\Users\\Haakon Kaurel\\pystock-data");
	}
}
