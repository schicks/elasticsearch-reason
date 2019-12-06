# BS-Elasticsearch

Well typed interface to the elasticsearch query dsl. Allows construction of queries as Reason types, which can be validated at compile time and produces javascript objects for querying that can be used through the elasticsearch-node package or directly through the rest interface.

## Running Tests

The tests rely on a local version of elasticsearch running in docker. To start the local elasticsearch, run `docker-compose up` and wait until elasticsearch is responding at `http://localhost:9200`. Then run `npm test`.