FROM erlang:alpine

WORKDIR /code

COPY rebar.config .
COPY run_test.sh .
COPY test test/
COPY src src/
RUN rebar3 compile

CMD ["sh", "run_test.sh"]
