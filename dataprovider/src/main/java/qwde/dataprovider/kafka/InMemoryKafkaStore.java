package qwde.dataprovider.kafka;

import com.salesforce.kafka.test.KafkaTestUtils;
import com.salesforce.kafka.test.junit5.SharedKafkaTestResource;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.TopicPartition;
import org.apache.kafka.common.serialization.Deserializer;
import org.apache.kafka.common.serialization.Serializer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

public class InMemoryKafkaStore {
    private final SharedKafkaTestResource sharedKafkaTestResource;
    private final KafkaTestUtils kafkaTestUtils;
    private static final Logger LOG = LoggerFactory.getLogger(InMemoryKafkaStore.class);

    public InMemoryKafkaStore() throws IOException {
        this.sharedKafkaTestResource = new SharedKafkaTestResource()
              .withBrokers(1)
              .withBrokerProperty("auto.create.topics.enable", "false");
        try {
            this.sharedKafkaTestResource.beforeAll(null);
        } catch (Exception exception) {
            throw new IOException(exception);
        }
        kafkaTestUtils = sharedKafkaTestResource.getKafkaTestUtils();
    }

    public void createTopic(String topicName) {
        this.kafkaTestUtils.createTopic(topicName, 1, (short) 1);
    }

    public <K, V> KafkaProducer<K, V> makeProducer(String name, Class<? extends Serializer<K>> keyClass, Class<? extends Serializer<V>> valClass) {
        final Properties producerProps = new Properties();
        producerProps.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, sharedKafkaTestResource.getKafkaBrokers().getBrokerById(1).getConnectString());
        producerProps.put(ProducerConfig.CLIENT_ID_CONFIG, name);
        producerProps.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, keyClass);
        producerProps.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, valClass);
        return new KafkaProducer<>(producerProps);
    }

    public <K, V> KafkaConsumer<K, V> makeConsumer(String name, String topic, Class<? extends Deserializer<K>> keyClass, Class<? extends Deserializer<V>> valClass) throws IOException {
        final Properties consumerProps = new Properties();
        consumerProps.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, sharedKafkaTestResource.getKafkaBrokers().getBrokerById(1).getConnectString());
        consumerProps.put(ConsumerConfig.CLIENT_ID_CONFIG, name);
        consumerProps.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, keyClass);
        consumerProps.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, valClass);
        KafkaConsumer<K, V> kafkaConsumer = new KafkaConsumer<>(consumerProps);
        if (kafkaConsumer.partitionsFor(topic) == null) {
            LOG.error("Unable to find partitions for {}. Is producer initialized?", topic);
            throw new IOException("Unable to find partitions for " + topic);
        }
        List<TopicPartition> tp = kafkaConsumer.partitionsFor(topic).stream().map(t -> new TopicPartition(topic, 0)).collect(Collectors.toList());
        kafkaConsumer.assign(tp);
        kafkaConsumer.seekToBeginning(tp);

        return kafkaConsumer;
    }
}
