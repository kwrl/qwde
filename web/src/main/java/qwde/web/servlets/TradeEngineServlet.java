package qwde.web.servlets;

import io.micronaut.core.convert.format.Format;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.trading.engine.TradeEngine;
import qwde.trading.model.Summary;
import qwde.trading.realtime.algorithm.TradingAlgorithm;

import java.io.IOException;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Controller(value = "/tradeengine", produces = MediaType.APPLICATION_JSON)
public final class TradeEngineServlet {
    private static final Logger LOG = LoggerFactory.getLogger(TradeEngineServlet.class);
    private static final int DEFAULT_VIEW_DAYS = 50;

    @Get("/{tickers}/{algorithms}/{fromDate}")
    public HttpResponse<?> doGet(String tickers, String algorithms, @Format("yyyyMMdd") LocalDate fromDate, @QueryValue @Format("yyyyMMdd") Optional<LocalDate> toDate) throws SQLException {
        if (toDate.isPresent() && toDate.get().isBefore(fromDate)) {
            return HttpResponse.badRequest("toDate is before fromDate!");
        }

        LocalDate endDate = toDate.orElse(fromDate.plusDays(DEFAULT_VIEW_DAYS));
        LOG.debug("Doing render with {}, {}, {}", tickers, fromDate, toDate);

        List<String> stockTickers = Arrays.asList(tickers.split(",")).stream().map(String::toUpperCase).collect(Collectors.toList());

        try {
            List<TradingAlgorithm.Type> algorithmTypes = new ArrayList<>();
            for (String alg : Arrays.asList(algorithms.split(","))) {
                algorithmTypes.add(TradingAlgorithm.Type.valueOf(alg.toUpperCase()));
            }

            return HttpResponse.ok(doTradeEngine(stockTickers, algorithmTypes, fromDate, endDate));
        } catch (IOException exception) {
            return HttpResponse.serverError("Something bad happened");
        }
    }

    private static Summary doTradeEngine(List<String> tickers, List<TradingAlgorithm.Type> algorithms, LocalDate fromDate, LocalDate endDate) throws IOException {
        for (TradingAlgorithm.Type type : algorithms) {
            TradeEngine.getInstance().addTradingAlgorithm(type.getInstance(tickers, 1e6));
        }

        return TradeEngine.getInstance().pollDataSql(tickers, fromDate, endDate);
    }
}
