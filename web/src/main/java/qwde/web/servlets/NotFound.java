package qwde.web.servlets;

import java.util.Collections;

import io.micronaut.http.HttpRequest;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.HttpStatus;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Error;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Random;

import qwde.web.plotly.PageRenderer;

@Controller(value = "/notfound", produces = MediaType.TEXT_HTML)
public class NotFound {
  private static final Logger LOG = LoggerFactory.getLogger(BollingerBrand.class);

  public NotFound() {
  }

  @Error(status = HttpStatus.NOT_FOUND, global = true)
  public HttpResponse notFound(HttpRequest request) {
    LOG.debug("HTTP 404: {}", request.getUri());
    return HttpResponse.ok(PageRenderer.renderPage("notfound.ftl", Collections.singletonMap("pageTitle", "404 not found")))
          .contentType(MediaType.TEXT_HTML);
  }
}
