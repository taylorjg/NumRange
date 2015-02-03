using System;
using System.Collections.Generic;
using System.Linq;
using Code;
using FsCheck;
using FsCheck.Fluent;
using FsCheckUtils;
using NUnit.Framework;

namespace PropertyTests
{
    [TestFixture]
    public class NumRangePropertyTests
    {
        private static readonly Configuration Configuration = Config.VerboseThrowOnFailure.ToConfiguration();

        [Test]
        public void ToNumListTest1()
        {
            Spec
                .For(GenNumRange, tuples =>
                {
                    var expandedTuples = ExpandTuples(tuples).ToList();
                    var input = MakeInput(tuples);
                    var actual = input.ToNumList();
                    return actual.SequenceEqual(expandedTuples);
                })
                .Check(Configuration);
        }

        private static IEnumerable<int> ExpandTuples(IEnumerable<Tuple<int, int>> tuples)
        {
            return tuples.SelectMany(t =>
            {
                var x = t.Item1;
                var y = t.Item2;
                return (x == y) ? Enumerable.Repeat(x, 1) : Enumerable.Range(x, y - x + 1);
            });
        }

        private static string MakeInput(IEnumerable<Tuple<int, int>> tuples)
        {
            var bits = tuples.Select(t =>
            {
                var x = t.Item1;
                var y = t.Item2;
                return (x == y) ? Convert.ToString(x) : string.Format("{0}-{1}", x, y);
            });
            return string.Join(",", bits);
        }

        private static Gen<Tuple<int, int>> GenTuple
        {
            get
            {
                return from n1 in Gen.choose(1, 50)
                       from n2 in Gen.choose(n1, 50)
                       select Tuple.Create(n1, n2);
            }
        }

        private static Gen<List<Tuple<int, int>>> GenNumRange
        {
            get { return GenTuple.MakeList(); }
        }
    }
}
