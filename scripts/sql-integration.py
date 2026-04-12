import pandas as pd
import sqlalchemy
import time

start_time = time.time()
print("🚀 Starting streaming upload...")

engine = sqlalchemy.create_engine(
    "mssql+pyodbc://@localhost\\SQLEXPRESS/crimeAnalysisDB?driver=ODBC+Driver+17+for+SQL+Server",
    fast_executemany=True
)

chunk_size = 1000

# ✅ READ CSV IN CHUNKS (NO MEMORY ISSUE)
for i, chunk in enumerate(pd.read_csv(
    r"C:\Users\MS994\Downloads\pds\crime_with_clusters.csv",
    chunksize=chunk_size
)):
    print(f"⏳ Processing chunk {i}...")

    # Clean column names
    chunk.columns = (
        chunk.columns
        .str.strip()
        .str.replace(' ', '_')
        .str.replace(r'[^0-9a-zA-Z_]', '', regex=True)
    )

    # Convert to string (safe)
    chunk = chunk.astype(str)

    chunk.to_sql(
        "crime_data",
        engine,
        if_exists='append' if i != 0 else 'replace',
        index=False
    )

    print(f"✅ Uploaded chunk {i}")

print("🎉 Upload completed!")
print(f"⏱ Time: {time.time() - start_time:.2f} sec")