// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Generic;
using System.Collections.Immutable;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

namespace Roslyn.Utilities
{
    public static class SpecializedTasks
    {
        public static readonly Task<bool> True = Task.FromResult(true);
        public static readonly Task<bool> False = Task.FromResult(false);
        public static readonly Task EmptyTask = Task.CompletedTask;

        public static Task<T> Default<T>()
        {
            return Empty<T>.Default;
        }

        public static Task<T> DefaultOrResult<T>(T value)
        {
            if (EqualityComparer<T>.Default.Equals(value, default))
            {
                return Default<T>();
            }

            return Task.FromResult(value);
        }

        public static Task<IReadOnlyList<T>> EmptyReadOnlyList<T>()
        {
            return Empty<T>.EmptyReadOnlyList;
        }

        public static Task<IList<T>> EmptyList<T>()
        {
            return Empty<T>.EmptyList;
        }

        private static class Empty<T>
        {
            public static readonly Task<T> Default = Task.FromResult<T>(default);
            public static readonly Task<ImmutableArray<T>> EmptyImmutableArray = Task.FromResult(ImmutableArray<T>.Empty);
            public static readonly Task<IList<T>> EmptyList = Task.FromResult(SpecializedCollections.EmptyList<T>());
            public static readonly Task<IReadOnlyList<T>> EmptyReadOnlyList = Task.FromResult(SpecializedCollections.EmptyReadOnlyList<T>());
        }

        private static class FromResultCache<T> where T : class
        {
            private static readonly ConditionalWeakTable<T, Task<T>> s_fromResultCache = new ConditionalWeakTable<T, Task<T>>();
            private static readonly ConditionalWeakTable<T, Task<T>>.CreateValueCallback s_taskCreationCallback = Task.FromResult<T>;

            public static Task<T> FromResult(T t)
            {
                return s_fromResultCache.GetValue(t, s_taskCreationCallback);
            }
        }
    }
}
