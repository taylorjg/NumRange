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
        private const string Separator = ",";

        [Test]
        public void ToNumListTest1()
        {
            Spec
                .For(GenNumRange1, tuples =>
                {
                    var inputString = FormatInputStringFromTuples(tuples);
                    var expected = ExpandTuples(tuples);
                    var actual = inputString.ToNumList();
                    return actual.SequenceEqual(expected);
                })
                .Check(Configuration);
        }

        [Test]
        public void ToNumListTest2()
        {
            Spec
                .For(GenNumRange2, tuples =>
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

        private static Gen<Tuple<int, int>> GenTupleSame
        {
            get
            {
                return from n1 in Gen.choose(1, 50)
                       select Tuple.Create(n1, n1);
            }
        }

        private static Gen<Tuple<int, int>> GenTupleDifferent
        {
            get
            {
                return from n1 in Gen.choose(1, 50)
                       from n2 in Gen.choose(n1, 50)
                       select Tuple.Create(n1, n2);
            }
        }

        private static Gen<List<Tuple<int, int>>> GenNumRange1
        {
            get { return GenTupleDifferent.MakeList(); }
        }

        private static Gen<List<Tuple<int, int>>> GenNumRange2
        {
            get
            {
                return Any
                    .WeighedGeneratorIn(
                        new WeightAndValue<Gen<Tuple<int, int>>>(40, GenTupleSame),
                        new WeightAndValue<Gen<Tuple<int, int>>>(60, GenTupleDifferent))
                    .MakeList();
            }
        }
    }
}
