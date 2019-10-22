package qwde.trading.realtime.algorithm;

import com.google.common.truth.Truth;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.serialization.StringSerializer;
import org.junit.jupiter.api.Test;
import qwde.dataprovider.kafka.InMemoryKafkaStore;
import qwde.dataprovider.kafka.serializer.StockTickerDeserializer;
import qwde.dataprovider.kafka.serializer.StockTickerSerializer;
import qwde.dataprovider.models.StockTicker;
import qwde.trading.engine.TradeEngine;

import java.io.IOException;

class BuyLowSellHighTest {
    @Test
    void testTradingRuns() throws IOException {
        TradeEngine.getInstance().addTradingAlgorithm(new BuyLowSellHigh("TWTR", 1e6));

        TradeEngine.Summary summary = TradeEngine.getInstance().pollData(store.makeConsumer("test", topic, StringDeserializer.class, StockTickerDeserializer.class));

        Truth.assertThat(summary.buyHistory).isNotEmpty();
    }
}
