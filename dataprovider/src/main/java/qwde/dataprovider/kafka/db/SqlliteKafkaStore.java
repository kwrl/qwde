package qwde.dataprovider.kafka.db;

import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.common.serialization.StringSerializer;
import qwde.analytics.db.StockDB;
import qwde.dataprovider.kafka.InMemoryKafkaStore;
import qwde.dataprovider.kafka.serializer.StockTickerSerializer;
import qwde.dataprovider.models.CompanyStockData;
import qwde.dataprovider.models.StockTicker;

import java.io.IOException;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.Collection;

public class SqlliteKafkaStore {
    public static KafkaConsumer<String, StockTicker> sqlliteKafkaStore(Collection<String> tickers, LocalDate fromDate, LocalDate toDate) throws IOException, SQLException {

        String topic = "streaming.stockticker.stockticker";

        InMemoryKafkaStore store = new InMemoryKafkaStore();
        store.createTopic(topic);
        KafkaProducer<String, StockTicker> kafkaProducer = store.makeProducer("test", StringSerializer.class, StockTickerSerializer.class);

        CompanyStockData stockData = StockDB.getCompanyData(tickers, fromDate, toDate);
    }
}
