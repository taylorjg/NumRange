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
        private static readonly Configuration Configuration =
            Config.VerboseThrowOnFailure
                .ToConfiguration()
                .WithMaxTest(500)
                .WithEndSize(200);

        private const string Separator = ",";

        [Test]
        public void ToNumListPropertyBasedTest1()
        {
            Spec
                .For(GenNumRangeAny, tuples =>
                {
                    var inputString = FormatInputStringFromTuples(tuples);
                    var expected = ExpandTuples(tuples);
                    var actual = inputString.ToNumList();
                    return actual.SequenceEqual(expected);
                })
                .Check(Configuration);
        }

        [Test]
        public void ToNumListPropertyBasedTest2()
        {
            Spec
                .For(GenNumRangeOrderedAndNonOverlapping, tuples =>
                {
                    var inputString = FormatInputStringFromTuples(tuples);
                    var expected = ExpandTuples(tuples);
                    var actual = inputString.ToNumList();
                    return actual.SequenceEqual(expected);
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

        private static string FormatInputStringFromTuples(IEnumerable<Tuple<int, int>> tuples)
        {
            return string.Join(Separator, tuples.Select(t =>
            {
                var x = t.Item1;
                var y = t.Item2;
                return (x == y) ? string.Format("{0}", x) : string.Format("{0}-{1}", x, y);
            }));
        }

        private static Gen<Tuple<int, int>> GenTupleAny
        {
            get
            {
                return from n1 in Gen.choose(1, 50)
                       from n2 in Gen.choose(n1, 50)
                       select Tuple.Create(n1, n2);
            }
        }

        private static Gen<List<Tuple<int, int>>> GenNumRangeAny
        {
            get { return GenTupleAny.MakeList(); }
        }

        private static Gen<Tuple<int, int>> GenTupleSingleNumber
        {
            get
            {
                return from n1 in Gen.choose(1, 50)
                       select Tuple.Create(n1, n1);
            }
        }

        private static Gen<Tuple<int, int>> GenTupleRange
        {
            get
            {
                return from n1 in Gen.choose(1, 50)
                       from n2 in Gen.choose(n1, 50)
                       where n1 != n2
                       select Tuple.Create(n1, n2);
            }
        }

        private static Gen<List<Tuple<int, int>>> GenNumRangeOrderedAndNonOverlapping
        {
            get
            {
                return Any
                    .WeighedGeneratorIn(
                        new WeightAndValue<Gen<Tuple<int, int>>>(60, GenTupleSingleNumber),
                        new WeightAndValue<Gen<Tuple<int, int>>>(40, GenTupleRange))
                    .MakeList()
                    .Select(OrderTuples)
                    .Where(TuplesDoNotOverlap);
            }
        }

        private static List<Tuple<int, int>> OrderTuples(List<Tuple<int, int>> tuples)
        {
            return tuples.OrderBy(t => t.Item1).ToList();
        }

        private static bool TuplesDoNotOverlap(List<Tuple<int, int>> tuples)
        {
            Func<Tuple<int, int>, Tuple<int, int>, bool> pairOfTuplesDoNotOverlap = (t1, t2) =>
            {
                System.Diagnostics.Debug.Assert(t1.Item2 >= t1.Item1);
                System.Diagnostics.Debug.Assert(t2.Item2 >= t2.Item1);
                System.Diagnostics.Debug.Assert(t2.Item1 >= t1.Item1);
                return t2.Item1 > t1.Item2;
            };

            if (tuples.Count < 2) return true;
            return pairOfTuplesDoNotOverlap(tuples[0], tuples[1]) && TuplesDoNotOverlap(tuples.Skip(1).ToList());
        }
    }
}
