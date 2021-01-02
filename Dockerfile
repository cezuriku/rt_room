FROM erlang:alpine

WORKDIR /code

COPY rebar.config .
COPY test test/
COPY src src/
RUN rebar3 compile

CMD ["rebar3", "eunit"]
