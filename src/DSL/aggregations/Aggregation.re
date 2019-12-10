open BucketAggregations
open MetricAggregations

type aggregation = 
| Bucket(string, bucketAggregation, list(aggregation))
| Metric(string, metricAggregation)

let rec cataAggregation = (fnBucket, fnMetric, agg) => switch (agg) {
| Bucket(name, config, subAggs) => fnBucket(name, config, List.map(cataAggregation(fnBucket, fnMetric), subAggs))
| Metric(name, config) => fnMetric(name, config)
}

let format = cataAggregation(
    (name, config, subAggs) => (name, BucketAggregations.serialize(config, subAggs)), 
    (name, config) => (name, MetricAggregations.serialize(config))
)
