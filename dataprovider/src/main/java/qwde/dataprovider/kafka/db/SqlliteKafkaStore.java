package qwde.dataprovider.kafka.db;

import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.serialization.StringSerializer;
import qwde.dataprovider.db.StockDB;
import qwde.dataprovider.kafka.InMemoryKafkaStore;
import qwde.dataprovider.kafka.serializer.StockTickerDeserializer;
import qwde.dataprovider.kafka.serializer.StockTickerSerializer;
import qwde.dataprovider.models.StockTicker;

import java.io.IOException;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.Collection;
import java.util.List;

public final class SqlliteKafkaStore {

    public static final String TOPIC = "streaming.stockticker.stockticker";

    private SqlliteKafkaStore() {
    }

    public static KafkaConsumer<String, StockTicker> sqlliteKafkaStore(Collection<String> tickers, LocalDate fromDate, LocalDate toDate) throws IOException, SQLException {

        InMemoryKafkaStore store = new InMemoryKafkaStore();
        store.createTopic(TOPIC);
        KafkaProducer<String, StockTicker> kafkaProducer = store.makeProducer("test", StringSerializer.class, StockTickerSerializer.class);

        List<StockTicker> stockData = StockDB.getCompanyData(tickers, fromDate, toDate);
        stockData.stream().forEach(t -> kafkaProducer.send(new ProducerRecord<>(TOPIC, t.symbol, t)));

        return store.makeConsumer("regularConsumer", TOPIC, StringDeserializer.class, StockTickerDeserializer.class);
    }
}
