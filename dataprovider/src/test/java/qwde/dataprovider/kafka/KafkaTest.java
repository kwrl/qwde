package qwde.dataprovider.kafka;


import com.flextrade.jfixture.JFixture;
import com.google.common.collect.ImmutableList;
import com.google.common.truth.Truth;
import kafka.Kafka;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import com.salesforce.kafka.test.KafkaBroker;
import com.salesforce.kafka.test.KafkaTestUtils;
import com.salesforce.kafka.test.junit5.SharedKafkaTestResource;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.common.Node;
import org.apache.kafka.common.serialization.Deserializer;
import org.apache.kafka.common.serialization.Serializer;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import qwde.dataprovider.models.StockTicker;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * Runs smoke tests against a PLAINTEXT enabled cluster.
 * @see AbstractSharedKafkaTestResourceTest for additional test case definitions.
 */
class KafkaTest extends SharedKafkaTestResource {
    /**
     * We have a single embedded kafka server that gets started when this test class is initialized.
     *
     * It's automatically started before any methods are run.
     * It's automatically stopped after all of the tests are completed.
     *
     * This example we start a cluster with 2 brokers (defaults to a single broker) and configure the brokers to
     * disable topic auto-creation.
     *
     * It must be scoped as 'public static' in order for the appropriate startup/shutdown hooks to be called on the extension.
     */
    @RegisterExtension
    public static final SharedKafkaTestResource sharedKafkaTestResource = new SharedKafkaTestResource()
            // Start a cluster with 2 brokers.
            .withBrokers(1)
            // Disable topic auto-creation.
            .withBrokerProperty("auto.create.topics.enable", "false");

    @Test
    void testStuff() {
        KafkaBroker kafkaBroker = sharedKafkaTestResource.getKafkaBrokers().getBrokerById(1);
        KafkaTestUtils kafkaTestUtils = sharedKafkaTestResource.getKafkaTestUtils();
        String topic = "streaming.stockticker.stockticker";

        kafkaTestUtils.createTopic(topic, 1, (short) 1);

        JFixture jFixture = new JFixture();
        StockTicker stockTicker = jFixture.create(StockTicker.class);

        Map<byte[], byte[]> kafkaMap = new HashMap<>();
        kafkaMap.put(SerializationUtils.serialize(stockTicker.timestamp), SerializationUtils.serialize(stockTicker));

        kafkaTestUtils.produceRecords(kafkaMap, topic, 0);

        List<ConsumerRecord<byte[], byte[]>> consumed = kafkaTestUtils.consumeAllRecordsFromTopic(topic);
        LocalDateTime ldt = SerializationUtils.deserialize(consumed.get(0).key());

        Truth.assertThat(ldt).isEqualTo(stockTicker.timestamp);
    }

    public static class LocalDateTimeSerializer implements Serializer<LocalDateTime> {
        @Override
        public byte[] serialize(String topic, LocalDateTime data) {
            return SerializationUtils.serialize(data);
        }
    }

    public static class LocalDateTimeDeserializer implements Deserializer<LocalDateTime> {
        @Override
        public LocalDateTime deserialize(String topic, byte[] data) {
            return SerializationUtils.deserialize(data);
        }
    }

    @Test
    void testMoreStuff() {
        KafkaBroker kafkaBroker = sharedKafkaTestResource.getKafkaBrokers().getBrokerById(1);
        KafkaTestUtils kafkaTestUtils = sharedKafkaTestResource.getKafkaTestUtils();
        String topic = "streaming.stockticker.stockticker2";

        kafkaTestUtils.createTopic(topic, 1, (short) 1);

        JFixture jFixture = new JFixture();
        StockTicker stockTicker = jFixture.create(StockTicker.class);

        Map<byte[], byte[]> kafkaMap = new HashMap<>();
        kafkaMap.put(SerializationUtils.serialize(stockTicker.timestamp), SerializationUtils.serialize(stockTicker));

        kafkaTestUtils.produceRecords(kafkaMap, topic, 0);
        System.out.println();
        final Properties props = new Properties();
        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, kafkaBroker.getConnectString().substring(12));
        props.put(ConsumerConfig.GROUP_ID_CONFIG, "KafkaExampleConsumer");
        props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, LocalDateTimeDeserializer.class.getName());
        props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        KafkaConsumer<LocalDateTime, String> kafkaConsumer = new KafkaConsumer<>(props);
        kafkaConsumer.subscribe(ImmutableList.of(topic));
        ConsumerRecords<LocalDateTime, String> consumed = kafkaConsumer.poll(Duration.ofSeconds(1));
        LocalDateTime ldt = ImmutableList.copyOf(consumed.records(topic)).get(0).key();

        Truth.assertThat(ldt).isEqualTo(stockTicker.timestamp);
    }
}
