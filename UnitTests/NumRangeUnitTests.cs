using Code;
using NUnit.Framework;

namespace UnitTests
{
    [TestFixture]
    internal class NumRangeUnitTests
    {
        [Test]
        public void NullStringReturnsEmptyList()
        {
            var actual = (null as string).ToNumList();
            Assert.That(actual, Is.Empty);
        }

        [Test]
        public void EmptyStringReturnsEmptyList()
        {
            var actual = "".ToNumList();
            Assert.That(actual, Is.Empty);
        }

        [Test]
        public void StringContainingOneNumReturnsAListContainingOneNum()
        {
            var actual = "12".ToNumList();
            Assert.That(actual, Is.EqualTo(new[] {12}));
        }

        [Test]
        public void StringContainingTwoNumsReturnsAListContainingTwoNums()
        {
            var actual = "12,13".ToNumList();
            Assert.That(actual, Is.EqualTo(new[] {12, 13}));
        }

        [Test]
        public void StringContainingOneRangeReturnsAListContainingOneRange()
        {
            var actual = "2-5".ToNumList();
            Assert.That(actual, Is.EqualTo(new[] {2, 3, 4, 5}));
        }

        [Test]
        public void ComplexTest()
        {
            var actual = "1-12,14,16".ToNumList();
            Assert.That(actual, Is.EqualTo(new[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16}));
        }
    }
}
