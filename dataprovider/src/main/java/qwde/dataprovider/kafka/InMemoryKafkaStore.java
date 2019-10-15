package qwde.dataprovider.kafka;

import com.salesforce.kafka.test.KafkaTestUtils;
import com.salesforce.kafka.test.junit5.SharedKafkaTestResource;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.Producer;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.Deserializer;
import org.apache.kafka.common.serialization.Serializer;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.serialization.StringSerializer;

import java.time.LocalDateTime;
import java.util.Properties;

public class InMemoryKafkaStore {
    private final SharedKafkaTestResource sharedKafkaTestResource = new SharedKafkaTestResource()
            .withBrokers(1)
            .withBrokerProperty("auto.create.topics.enable", "false");
    private final KafkaTestUtils kafkaTestUtils = sharedKafkaTestResource.getKafkaTestUtils();

    public void createTopic(String topicName) {
        this.kafkaTestUtils.createTopic(topicName, 1, (short) 1);
    }

    public <K, V> KafkaProducer<K, V> makeProducer(String name, Class<K> keyClass, Class<V> valClass) {
        final Properties producerProps = new Properties();
        producerProps.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, sharedKafkaTestResource.getKafkaBrokers().getBrokerById(1).getConnectString());
        producerProps.put(ProducerConfig.CLIENT_ID_CONFIG, name);
        producerProps.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, keyClass);
        producerProps.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, valClass);
        return new KafkaProducer<>(producerProps);
    }

    public <K, V> KafkaConsumer<K, V> makeConsumer(String name, Class<K> keyClass, Class<V> valClass) {
        final Properties consumerProps = new Properties();
        consumerProps.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, sharedKafkaTestResource.getKafkaBrokers().getBrokerById(1).getConnectString());
        consumerProps.put(ConsumerConfig.CLIENT_ID_CONFIG, name);
        consumerProps.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, keyClass);
        consumerProps.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, valClass);
        return new KafkaConsumer<>(consumerProps);
    }
}
