package qwde.trading.realtime.algorithm;

import com.google.common.truth.Truth;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.serialization.StringSerializer;
import org.junit.jupiter.api.Test;
import qwde.dataprovider.kafka.InMemoryKafkaStore;
import qwde.dataprovider.kafka.db.SqlliteKafkaStore;
import qwde.dataprovider.kafka.serializer.LocalDateTimeDeserializer;
import qwde.dataprovider.kafka.serializer.StockTickerDeserializer;
import qwde.dataprovider.kafka.serializer.StockTickerSerializer;
import qwde.dataprovider.models.StockTicker;
import qwde.trading.engine.TradeEngine;

import java.io.IOException;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.Collections;

class BuyLowSellHighTest {
    @Test
    void testTradingRuns() throws IOException, SQLException {
        TradeEngine.getInstance().addTradingAlgorithm(new BuyLowSellHigh("TWTR", 1e6));
        KafkaConsumer<String, StockTicker> kafkaConsumer = SqlliteKafkaStore.sqlliteKafkaStore(Collections.singleton("TWTR"), LocalDate.of(2014, 01, 01), LocalDate.of(2015, 01, 01));

        TradeEngine.Summary summary = TradeEngine.getInstance().pollData(kafkaConsumer, SqlliteKafkaStore.TOPIC);

        Truth.assertThat(summary.buyHistory).isNotEmpty();
    }
}
