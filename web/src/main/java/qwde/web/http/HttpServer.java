package qwde.web.http;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import qwde.web.servlets.IndexServlet;
import qwde.web.servlets.SimpleMovingAverage;
import qwde.web.util.FileUtil;

public class HttpServer implements Runnable {
  private static Logger logger = LoggerFactory.getLogger(HttpServer.class);

  private Socket client = null;
  private BufferedReader inClient = null;
  private DataOutputStream outClient = null;

  public HttpServer(Socket cl) {
    client = cl;
  }

  public Map<String, List<String>> getQueries(String httpQuery) {
    if (httpQuery.indexOf("?") < 0) {
      return Collections.emptyMap();
    }

    return Arrays.stream(httpQuery.substring(httpQuery.indexOf("?") + 1).split("&"))
      .map(this::splitQueryParameter)
      .collect(Collectors.groupingBy(SimpleImmutableEntry::getKey, LinkedHashMap::new, Collectors.mapping(Map.Entry::getValue, Collectors.toList())));
  }

  public SimpleImmutableEntry<String, String> splitQueryParameter(String it) {
    final int idx = it.indexOf("=");
    final String key = idx > 0 ? it.substring(0, idx) : it;
    final String value = idx > 0 && it.length() > idx + 1 ? it.substring(idx + 1) : null;
    return new SimpleImmutableEntry<>(key, value);
  }

  @Override
  public void run() {
    try {
      logger.debug("The Client {}:{} is connected", client.getInetAddress(), client.getPort());

      inClient = new BufferedReader(new InputStreamReader(client.getInputStream()));
      outClient = new DataOutputStream(client.getOutputStream());

      String requestString = inClient.readLine();
      String headerLine = requestString;

      if (headerLine == null) {
        return;
      }

      StringTokenizer tokenizer = new StringTokenizer(headerLine);
      String httpMethod = tokenizer.nextToken();
      String httpQueryString = tokenizer.nextToken();

      while (inClient.ready()) {
        logger.trace(requestString);
        requestString = inClient.readLine();
      }

      Map<String, List<String>> urlMapping = getQueries(httpQueryString);

      if (httpMethod.equals("GET")) {
        if (httpQueryString.equals("/")) {
          sendResponse(200, IndexServlet.doGet(urlMapping));
        } else if (httpQueryString.startsWith("/sma")) {
          sendResponse(200, SimpleMovingAverage.doGet(urlMapping));
        } else if (httpQueryString.startsWith("/plotly-latest.min.js")) {
          try {
            sendResponse(200, FileUtil.getResourceFile("plotly-latest.min.js"));
          } catch (IOException exception) {
            logger.debug("wtf?", exception);
            sendResponse(404, ":(");
          }
        } else {
          sendResponse(404, "<b>The Requested resource not found.</b>");
        }
      } else {
        sendResponse(404, "<b>The Requested resource not found.</b>");
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  public void sendResponse(int statusCode, String responseString) throws Exception {
    String statusLine = null;
    String serverdetails = "java.net.ServerSocket";
    String contentLengthLine = null;
    String contentTypeLine = "Content-Type: text/html\r\n";

    if (statusCode == 200) {
      statusLine = Status.HTTP_200;
    } else {
      statusLine = Status.HTTP_404;
    }

    responseString = "<html><title>HTTP Server in Java</title><body>" + responseString + "</body></html>";
    contentLengthLine = "Content-Length" + responseString.length() + "\r\n";

    outClient.writeBytes(statusLine);
    outClient.writeBytes(serverdetails);
    outClient.writeBytes(contentTypeLine);
    outClient.writeBytes(contentLengthLine);
    outClient.writeBytes("Connection: close\r\n");

    /** adding the new line between header and body */
    outClient.writeBytes("\r\n");

    outClient.writeBytes(responseString);

    outClient.close();
  }

  private static class Status {
    public static final String HTTP_200 = "HTTP/1.1 200 OK\r\n";
    public static final String HTTP_404 = "HTTP/1.1 404 Not Found\r\n";
  }
}
