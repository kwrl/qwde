package qwde.web.servlets;

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

@Controller(value = "/notfound", produces = MediaType.TEXT_HTML)
public class NotFound {
  private static final Logger LOG = LoggerFactory.getLogger(BollingerBrand.class);

  public NotFound() {
  }

  private static ArrayList<String> theHolySource = new ArrayList<>() {{
  }};

  @Error(status = HttpStatus.NOT_FOUND, global = true)
  public HttpResponse notFound(HttpRequest request) {
    LOG.debug("HTTP 404: {}", request.getUri());
    return HttpResponse.ok("<h2>404</h2><img src=\"" + theHolySource.get(new Random().nextInt(theHolySource.size())) + "\" alt=\"appropriate gif\" >")
          .contentType(MediaType.TEXT_HTML);
  }
}
