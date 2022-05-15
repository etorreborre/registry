import AutoDiscoveredSpecs (tests)
import Protolude
import Test.Tasty.Extensions

main = tests >>= defaultMain . groupByModuleName
