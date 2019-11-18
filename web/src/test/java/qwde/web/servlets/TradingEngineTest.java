package qwde.web.servlets;

import com.google.common.truth.Truth;
import io.micronaut.http.HttpRequest;
import io.micronaut.http.client.RxHttpClient;
import io.micronaut.http.client.annotation.Client;
import io.micronaut.test.annotation.MicronautTest;
import org.junit.jupiter.api.Test;
import qwde.trading.model.Summary;
import qwde.web.servlets.configuration.TradeEngineConfiguration;

import javax.inject.Inject;

@MicronautTest
class TradingEngineTest {
    @Client(value = "/", configuration = TradeEngineConfiguration.class)
    @Inject
    RxHttpClient client;

    @Test
    void tradingengine_buylowselltwtr20150102_makesPurchase() {
        Summary summary = client.toBlocking().retrieve(HttpRequest.GET("/tradeengine/twtr/buylowsellhigh/20150102"), Summary.class);

        Truth.assertThat(summary.buyHistory.size()).isGreaterThan(0);
    }
}
