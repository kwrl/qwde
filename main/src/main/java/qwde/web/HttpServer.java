package qwde.web;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.StringTokenizer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import qwde.servlets.IndexServlet;
import qwde.servlets.SharkToothServlet;

public class HttpServer implements Runnable {
  private static Logger logger = LoggerFactory.getLogger(HttpServer.class);

  private Socket client = null;
  private BufferedReader inClient = null;
  private DataOutputStream outClient = null;

  public HttpServer(Socket cl) {
    client = cl;
  }

  @Override
  public void run() {
    try {
      logger.debug("The Client " + client.getInetAddress() + ":" + client.getPort() + " is connected");

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

      if (httpMethod.equals("GET")) {
        if (httpQueryString.equals("/")) {
          sendResponse(200, IndexServlet.doGet());
        } else if (httpQueryString.startsWith("/sharktooth")) {
          sendResponse(200, SharkToothServlet.doGet());
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
