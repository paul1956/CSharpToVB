using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Text;
using Utilities;

public static class RuntimeMetadataReferenceResolver
{
    public static ImmutableDictionary<string, string> GetTrustedPlatformAssemblyMap()
    {
        var set = ImmutableDictionary.CreateBuilder<string, string>(StringComparer.OrdinalIgnoreCase);

        if (CoreClrShim.AppContext.GetData?.Invoke("TRUSTED_PLATFORM_ASSEMBLIES") is string paths)
        {
            foreach (var path in paths.Split(Path.PathSeparator))
            {
                if (Path.GetExtension(path) == ".dll")
                {
                    string fileName = Path.GetFileNameWithoutExtension(path);
                    if (fileName.EndsWith(".ni", StringComparison.OrdinalIgnoreCase))
                    {
                        fileName = fileName.Substring(0, fileName.Length - ".ni".Length);
                    }

                    // last one wins:
                    set[fileName] = path;
                }
            }
        }
        return set.ToImmutable();
    }
}
