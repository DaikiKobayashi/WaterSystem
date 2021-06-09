using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace KDaili.WaterSystem
{
    [CreateAssetMenu(menuName = "WaterData/WaterResources", order = 0)]
    public class WaterResources : ScriptableObject
    {
        public Material defaultSeaMaterial;
        public Mesh[] defaultWaterMeshes;
    }
}