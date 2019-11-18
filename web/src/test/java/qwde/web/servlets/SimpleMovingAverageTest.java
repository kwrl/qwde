package qwde.web.servlets;

import com.google.common.truth.Truth;
import io.micronaut.http.HttpRequest;
import io.micronaut.http.client.RxHttpClient;
import io.micronaut.http.client.annotation.Client;
import io.micronaut.test.annotation.MicronautTest;
import org.junit.jupiter.api.Test;

import javax.inject.Inject;

@MicronautTest
class SimpleMovingAverageTest {

    @Inject
    @Client("/")
    RxHttpClient client;

    @Test
    void sma_twtr20150102_14lines() {
        SimpleMovingAverage.Data sma = client.toBlocking().retrieve(HttpRequest.GET("/sma/twtr/20150102"), SimpleMovingAverage.Data.class);

        Truth.assertThat(sma.prices.size()).isEqualTo(14);
    }
}
