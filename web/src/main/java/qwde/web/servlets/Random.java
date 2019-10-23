package qwde.web.servlets;

import com.google.common.collect.ImmutableMap;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.reactivex.Single;
import qwde.web.plotly.FigureTemplate;
import qwde.web.plotly.LinePlotRenderer;
import qwde.web.plotly.PageRenderer;
import tech.tablesaw.plotly.traces.ScatterTrace;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;

@Controller(value = "/random", produces = MediaType.TEXT_HTML)
public final class Random {
    @Get("/")
    public Single<String> doGet() {
        List<Double> data = DoubleStream
                .generate(ThreadLocalRandom.current()::nextDouble)
                .limit(100)
                .boxed()
                .collect(Collectors.toList());

        List<Double> data2 = DoubleStream
                .generate(ThreadLocalRandom.current()::nextDouble)
                .limit(100)
                .boxed()
                .collect(Collectors.toList());

        return Single.just(PageRenderer.renderPage("graphpage.ftl", ImmutableMap.of("pageTitle", "Random stock data", "figures", Arrays.asList(
                new FigureTemplate(LinePlotRenderer.scatterPlot(Collections.singleton(LinePlotRenderer.genScatterPlot(data, "rand")), ScatterTrace.class, "A 2d scatterplot chart", "x", "y"), "Randomized stock data", "$$x_{0..n} = rand()$$Refreshing the page refreshes the data, too."),
                new FigureTemplate(LinePlotRenderer.scatterPlot(Collections.singleton(LinePlotRenderer.genScatterPlot(data2, "rand")), ScatterTrace.class, "Another 2d scatterplot chart", "x", "y"), "Randomized stock data v2", "$$x_{0..n} = rand()$$Refreshing the page refreshes the data, too.")
        ))));
    }
}
